package com.cf.parking.services.service;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.ParkingFreePOMapper;
import com.cf.parking.dao.mapper.ParkingSpacePOMapper;
import com.cf.parking.dao.po.ParkingFreePO;
import com.cf.parking.dao.po.ParkingSpacePO;
import com.cf.parking.dao.query.ParkingFreeQuery;
import com.cf.parking.facade.bo.ParkingFreeBO;
import com.cf.parking.facade.bo.ParkingSpaceBO;
import com.cf.parking.facade.bo.PlatePersonBO;
import com.cf.parking.facade.dto.ParkingFreeDTO;
import com.cf.parking.facade.dto.ParkingSpaceDTO;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.support.utils.DingAlarmUtils;
import com.hikvision.artemis.sdk.ArtemisHttpUtil;
import com.hikvision.artemis.sdk.config.ArtemisConfig;
import com.hikvision.artemis.sdk.constant.Constants;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.*;

@Slf4j
@Service
public class HikvisionService implements InitializingBean {

    static {
        //设置超时时间20秒
        Constants.DEFAULT_TIMEOUT = 20000;
        date = DateFormatUtils.format(System.currentTimeMillis(), "yyyy-MM-dd");
    }

    private static String date;

    private static Map<String, PlatePersonBO> personNameMap = new HashMap<>();
    private static Map<String, String> plateNoMap = new HashMap<>();

    @Resource
    private ParkingSpacePOMapper parkingSpacePOMapper;

    @Resource
    private ParkingFreePOMapper parkingFreePOMapper;

    @Override
    public void afterPropertiesSet() throws Exception {
        initPersonName();
    }

    private void initPersonName() {
        List<ParkingSpacePO> list = parkingSpacePOMapper.getAllTempCarInRecords();
        for (ParkingSpacePO parkingSpaceBO : list) {
            PlatePersonBO bo = new PlatePersonBO();
            bo.setPersonId(parkingSpaceBO.getPersonId());
            bo.setPersonName(parkingSpaceBO.getPersonName());
            personNameMap.put(parkingSpaceBO.getPlateNo(), bo);
        }
    }

    private void setPlateNoMap(Long pageNo) {
        Result apiResult = getApiResult("/api/resource/v2/vehicle/advance/vehicleList", pageNo, 1000L);
        JSONObject result = JSONObject.parseObject(apiResult.getData().toString());
        JSONObject data = result.getJSONObject("data");
        JSONArray list = data.getJSONArray("list");
        for (int i = 0; i < list.size(); i++) {
            JSONObject eachObj = list.getJSONObject(i);
            String plateNo = eachObj.getString("plateNo");
            String personId = eachObj.getString("personId");
            if (StringUtils.isNotBlank(plateNo) && StringUtils.isNotBlank(personId)) {
                plateNoMap.put(personId, plateNo);
            }
        }
    }

    private List<ParkingSpacePO> setStateList(Long pageNo) {
        List<ParkingSpacePO> stateList = new ArrayList();
        Result apiResult = getApiResult("/api/pms/v1/tempCarInRecords/page", pageNo, 1000L);
        JSONObject result = JSONObject.parseObject(apiResult.getData().toString());
        JSONObject data = result.getJSONObject("data");
        JSONArray list = data.getJSONArray("list");
        for (int i = 0; i < list.size(); i++) {
            JSONObject eachObj = list.getJSONObject(i);
            String parkTime = eachObj.getString("parkTime");
            if (StringUtils.isNotBlank(parkTime)) {
                ParkingSpacePO ps = new ParkingSpacePO();
                ps.setPlateNo(eachObj.getString("plateNo"));
                ps.setState(1);
                ps.setParkTime(parkTime);
                ps.setInTime(eachObj.getDate("inTime"));
                stateList.add(ps);
            }
        }
        return stateList;
    }


    /**
     * 通用接口请求
     *
     * @param apiPath
     * @param pageNo
     * @param pageSize
     * @return
     */
    public Result getApiResult(String apiPath, Long pageNo, Long pageSize) {
        /**
         * STEP1：设置平台参数，根据实际情况,设置host appkey appsecret 三个参数.
         */
        ArtemisConfig artemisConfig = new ArtemisConfig("60.191.107.14:2345", "23984426", "u51bdIluCwbZb9U8CtoF");

        /**
         * STEP2：设置OpenAPI接口的上下文
         */
        final String ARTEMIS_PATH = "/artemis";

        /**
         * STEP3：设置接口的URI地址
         */
        final String previewURLsApi = ARTEMIS_PATH + apiPath;
        Map<String, String> path = new HashMap<String, String>(2) {
            {
                put("https://", previewURLsApi);//根据现场环境部署确认是http还是https
            }
        };

        /**
         * STEP4：设置参数提交方式
         */
        String contentType = "application/json";

        /**
         * STEP5：组装请求参数
         */
        JSONObject jsonBody = new JSONObject();
        jsonBody.put("cameraIndexCode", "748d84750e3a4a5bbad3cd4af9ed5101");
        jsonBody.put("streamType", 0);
        jsonBody.put("protocol", "rtsp");
        jsonBody.put("transmode", 1);
        if (pageNo != null) {
            jsonBody.put("pageNo", pageNo);
        }
        if (pageSize != null) {
            jsonBody.put("pageSize", pageSize);
        }
        jsonBody.put("expand", "streamform=ps");
        String body = jsonBody.toJSONString();

        log.info("doPostStringArtemis,path={},param={}", body);
        long tm = System.currentTimeMillis();
        try {
            String result = ArtemisHttpUtil.doPostStringArtemis(artemisConfig, path, body, null, null, contentType, null);
            log.info("doPostStringArtemis,result={}", result);

            return Result.buildSuccessResult(result);
        } catch (Exception e) {
            log.error("getApiResultErr", e);
            DingAlarmUtils.alarmException("params[path=" + path + ",pageNo=" + pageNo + ",pageSize=" + pageSize + "],rt=" + (System.currentTimeMillis() - tm) + ",err=" + e.getMessage());
            throw new RuntimeException(e);
        }
    }

    /**
     * 过车记录
     *
     * @param apiPath
     * @param pageNo
     * @param pageSize
     * @return
     */
    public Result getCrossRecords(String apiPath, Long pageNo, Long pageSize) {
//        initPersonName();
        Result apiResult = getApiResult(apiPath, pageNo, pageSize);

        JSONObject result = JSONObject.parseObject(apiResult.getData().toString());
        JSONObject data = result.getJSONObject("data");
        if(data.get("list") == null){
            return Result.buildSuccessResult();
        }
        JSONArray list = data.getJSONArray("list");
        for (int i = 0; i < list.size(); i++) {
            JSONObject eachObj = list.getJSONObject(i);
            String plateNo = eachObj.getString("plateNo");
            PlatePersonBO bo = personNameMap.get(plateNo);
            if (bo != null) {
                eachObj.put("personName", bo.getPersonName());
            }
        }
        return Result.buildSuccessResult(data);
    }

    /**
     * 场内车停车信息
     *
     * @param param
     * @return
     */
    public PageResponse<ParkingSpaceBO> getTempCarInRecords(ParkingSpaceDTO param) {
        ParkingSpacePO ps = new ParkingSpacePO();
        BeanConvertorUtils.copy(param, ps);

        Page page = new Page();
        page.setCurrent(param.getPageNo());
        page.setSize(param.getPageSize());
        IPage<ParkingSpacePO> tempCarInRecords = parkingSpacePOMapper.getTempCarInRecords(page, ps);
        List<ParkingSpacePO> records = tempCarInRecords.getRecords();
        List<ParkingSpaceBO> parkingSpaceBOS = BeanConvertorUtils.copyList(records, ParkingSpaceBO.class);
        return new PageResponse<>(parkingSpaceBOS, param.getPageNo(), tempCarInRecords.getTotal(), param.getPageSize());
    }

    public Result initPlateNo() {
        setPlateNoMap(1L);
        setPlateNoMap(2L);
        List<ParkingSpacePO> list = new ArrayList<>();
        for (String key : plateNoMap.keySet()) {
            ParkingSpacePO parkingSpacePO = new ParkingSpacePO();
            parkingSpacePO.setPlateNo(plateNoMap.get(key));
            parkingSpacePO.setPersonId(key);
            list.add(parkingSpacePO);
        }
        parkingSpacePOMapper.updateByPersonIdSelective(list);
        return Result.buildSuccessResult();
    }

    public Result sycnData() {
        List<ParkingSpacePO> stateList = setStateList(1L);
        parkingSpacePOMapper.clearAll();
        parkingSpacePOMapper.updateByPlateNoSelective(stateList);
        return Result.buildSuccessResult();
    }

    public List<ParkingFreeBO> getList(ParkingFreeDTO param) {
        ParkingFreeQuery query = new ParkingFreeQuery();
        BeanConvertorUtils.copy(param, query);
        List<ParkingFreePO> list = this.parkingFreePOMapper.getList(query);
        return BeanConvertorUtils.copyList(list, ParkingFreeBO.class);
    }

    public void getParkingFree() {
        List<ParkingFreePO> freeList = new ArrayList();
        Result apiResult = getApiResult("/api/pms/v1/park/remainSpaceNum", 1L, 1000L);
        JSONObject result = JSONObject.parseObject(apiResult.getData().toString());
        JSONArray list = result.getJSONArray("data");
        for (int i = 0; i < list.size(); i++) {
            JSONObject eachObj = list.getJSONObject(i);
            ParkingFreePO pf = new ParkingFreePO();
            pf.setStatDate(new Date());
            pf.setFreeNum(eachObj.getInteger("leftPlace"));
            String parkName = eachObj.getString("parkName");
            if (parkName.equals("地下停车场")) {
                pf.setParkFlag(1);
            } else if (parkName.equals("组装车间4F停车库")) {
                pf.setParkFlag(2);
            } else if (parkName.equals("组装车间1F")) {
                pf.setParkFlag(3);
            } else {
                continue;
            }
            freeList.add(pf);
        }
        parkingFreePOMapper.insertList(freeList);
    }
}
