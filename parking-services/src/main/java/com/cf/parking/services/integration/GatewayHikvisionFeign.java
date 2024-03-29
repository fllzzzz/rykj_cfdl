package com.cf.parking.services.integration;


import com.cf.parking.facade.dto.*;
import com.cf.support.result.PageResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.util.List;
import java.util.Map;

/**
 * @Author caiChengYu
 * @Date 9:16 2022/7/25
 * @Version 1.0
 **/
@FeignClient(url = "${gateway.url}" + "/hikvision", name = "gatewayHikvisionFeign", configuration = ClientConfiguration.class)
public interface GatewayHikvisionFeign {

    /**
     * 查询场内车停车信息
     * https://open.hikvision.com/docs/docId?productId=5c67f1e2f05948198c909700&version=%2Ff8356830af1d40f3b1da7db12baa47af&tagPath=API%E5%88%97%E8%A1%A8-%E8%BD%A6%E8%BE%86%E7%AE%A1%E6%8E%A7-%E5%81%9C%E8%BD%A6%E5%9C%BA%E5%8A%9F%E8%83%BD%E6%8E%A5%E5%8F%A3#c4292e21
     *
     * @return
     */
    @RequestMapping(method = RequestMethod.POST, value = "/{path}")
    HikvisionResult<PageResponse<CarInRecordDTO>> tempCarInRecords(@PathVariable("path") String path, CarInRecordQueryDTO carInRecordQueryDTO);

    /**
     * 批量删除黑名单车辆（取消车辆布控）
     * https://open.hikvision.com/docs/docId?productId=5c67f1e2f05948198c909700&version=%2Ff95e951cefc54578b523d1738f65f0a1&tagPath=API%E5%88%97%E8%A1%A8-%E8%BD%A6%E8%BE%86%E7%AE%A1%E6%8E%A7-%E5%81%9C%E8%BD%A6%E5%9C%BA%E5%8A%9F%E8%83%BD#dbb000d8
     *
     * @param path
     * @param alarmSysCodeDTO
     * @return
     */
    @RequestMapping(method = RequestMethod.POST, value = "/{path}")
    HikvisionResult<Map<String, Object>> blackListDeletion(@PathVariable("path") String path, BlackListBatchDelAlarmSyscodeDTO alarmSysCodeDTO);

    /**
     * 查询车辆包期
     * https://open.hikvision.com/docs/docId?productId=5c67f1e2f05948198c909700&version=%2Ff8356830af1d40f3b1da7db12baa47af&tagPath=API%E5%88%97%E8%A1%A8-%E8%BD%A6%E8%BE%86%E7%AE%A1%E6%8E%A7-%E5%81%9C%E8%BD%A6%E5%9C%BA%E5%8A%9F%E8%83%BD%E6%8E%A5%E5%8F%A3#bb7cb58c
     *
     * @param path             path
     * @param userSpaceSyncDTO userSpaceSyncDTO
     * @return
     */
    @RequestMapping(method = RequestMethod.POST, value = "/{path}")
    HikvisionResult<PageResponse<UserSpaceDTO>> userSpaceDataSync(@PathVariable("path") String path, UserSpaceSyncDTO userSpaceSyncDTO);

    /**
     * 查询过车记录
     * https://open.hikvision.com/docs/docId?productId=5c67f1e2f05948198c909700&version=%2Ff8356830af1d40f3b1da7db12baa47af&tagPath=API%E5%88%97%E8%A1%A8-%E8%BD%A6%E8%BE%86%E7%AE%A1%E6%8E%A7-%E5%81%9C%E8%BD%A6%E5%9C%BA%E5%8A%9F%E8%83%BD%E6%8E%A5%E5%8F%A3#e6fad46f
     *
     * @return
     */
    @RequestMapping(method = RequestMethod.POST, value = "/{path}")
    HikvisionResult<PageResponse<CrossRecordsDTO>> crossRecords(@PathVariable("path") String path, CrossRecordsQueryDTO crossRecordsQueryDTO);

    /**
     * 添加黑名单
     * https://open.hikvision.com/docs/docId?productId=5c67f1e2f05948198c909700&version=%2Ff95e951cefc54578b523d1738f65f0a1&tagPath=API%E5%88%97%E8%A1%A8-%E8%BD%A6%E8%BE%86%E7%AE%A1%E6%8E%A7-%E5%81%9C%E8%BD%A6%E5%9C%BA%E5%8A%9F%E8%83%BD#afbb27a7
     *
     * @param path
     * @param blackListAdditionDTO
     * @return
     */
    @RequestMapping(method = RequestMethod.POST, value = "/{path}")
    HikvisionResult<BlackListAlarmSyscodeDTO> blackListAddition(@PathVariable("path") String path, BlackListAdditionDTO blackListAdditionDTO);

    /**
     * 车辆充值
     * https://open.hikvision.com/docs/docId?productId=5c67f1e2f05948198c909700&version=%2F60df74fdc6f24041ac3d2d7f81c32325&tagPath=API%E5%88%97%E8%A1%A8-%E8%BD%A6%E8%BE%86%E7%AE%A1%E6%8E%A7-%E5%81%9C%E8%BD%A6%E5%9C%BA%E5%8A%9F%E8%83%BD%E6%8E%A5%E5%8F%A3#b6e56e55
     *
     * @param path
     * @param carChargeDTO
     * @return
     */
    @RequestMapping(method = RequestMethod.POST, value = "/{path}")
    HikvisionResult<String> setCarCharge(@PathVariable("path") String path, CarChargeDTO carChargeDTO);

    /**
     * 取消车辆包期
     * https://open.hikvision.com/docs/docId?productId=5c67f1e2f05948198c909700&version=%2F60df74fdc6f24041ac3d2d7f81c32325&tagPath=API%E5%88%97%E8%A1%A8-%E8%BD%A6%E8%BE%86%E7%AE%A1%E6%8E%A7-%E5%81%9C%E8%BD%A6%E5%9C%BA%E5%8A%9F%E8%83%BD%E6%8E%A5%E5%8F%A3#d95589de
     *
     * @param path
     * @param carChargeDelDTO
     * @return
     */
    @RequestMapping(method = RequestMethod.POST, value = "/{path}")
    HikvisionResult<String> delCarCharge(@PathVariable("path") String path, CarChargeDelDTO carChargeDelDTO);

    /**
     * 获取停车库列表
     * https://open.hikvision.com/docs/docId?productId=5c67f1e2f05948198c909700&version=%2F60df74fdc6f24041ac3d2d7f81c32325&tagPath=API%E5%88%97%E8%A1%A8-%E8%BD%A6%E8%BE%86%E7%AE%A1%E6%8E%A7-%E8%BD%A6%E8%BE%86%E5%8F%8A%E8%BD%A6%E5%BA%93%E4%BF%A1%E6%81%AF%E6%8E%A5%E5%8F%A3#d93e4991
     *
     * @param path
     * @param getParkListDTO
     * @return
     */
    @RequestMapping(method = RequestMethod.POST, value = "/{path}")
    HikvisionResult<List<ParkListDTO>> getParkList(@PathVariable("path") String path, GetParkListDTO getParkListDTO);

    /**
     * 查询停车库剩余车位数
     * https://open.hikvision.com/docs/docId?productId=5c67f1e2f05948198c909700&version=%2F60df74fdc6f24041ac3d2d7f81c32325&tagPath=API%E5%88%97%E8%A1%A8-%E8%BD%A6%E8%BE%86%E7%AE%A1%E6%8E%A7-%E5%81%9C%E8%BD%A6%E5%9C%BA%E5%8A%9F%E8%83%BD%E6%8E%A5%E5%8F%A3#c0be693e
     *
     * @param path
     * @param parkSyscodeDTO
     * @return
     */
    @RequestMapping(method = RequestMethod.POST, value = "/{path}")
    HikvisionResult<List<SpaceNumDTO>> remainSpaceNum(@PathVariable("path") String path, ParkSyscodeDTO parkSyscodeDTO);
}
