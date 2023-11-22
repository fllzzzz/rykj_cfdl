package com.cf.parking.services.facade.impl;

import java.util.*;
import java.util.stream.Collectors;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.LotteryRuleAssignMapper;
import com.cf.parking.dao.po.*;
import com.cf.parking.facade.bo.LotteryRuleAssignBO;
import com.cf.parking.facade.bo.LotteryRuleAssignExportBO;
import com.cf.parking.facade.dto.LotteryRuleAssignDTO;
import com.cf.parking.facade.dto.LotteryRuleAssignOptDTO;
import com.cf.parking.facade.facade.LotteryRuleAssignFacade;
import com.cf.parking.services.enums.RuleAssignTypeEnum;
import com.cf.parking.services.service.*;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.bean.IdWorker;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;

/**
 * 摇号规则-停车场分配Service业务层处理
 * 
 * @author
 * @date 2023-09-05
 */
@Slf4j
@Service
public class LotteryRuleAssignFacadeImpl implements LotteryRuleAssignFacade
{
    @Resource
    private LotteryRuleAssignMapper mapper;

    @Resource
    private LotteryRuleRoundService lotteryRuleRoundService;

    @Resource
    private DepartmentService departmentService;

    @Resource
    private UserProfileService userProfileService;

    @Resource
    private IdWorker idWorker;

    @Resource
    private ParkingLotService parkingLotService;

    @Resource
    private EmployeeService employeeService;

    /**
     * 查询摇号规则-停车场分配列表
     * @param dto
     * @return
     */
    @Override
    public PageResponse<LotteryRuleAssignBO> getLotteryRuleAssignList(LotteryRuleAssignDTO dto) {
        Page<LotteryRuleAssignPO> page = PageUtils.toPage(dto);

        Page<LotteryRuleAssignPO> poPage = mapper.selectPage(page, new LambdaQueryWrapper<LotteryRuleAssignPO>()
                .eq(StringUtils.isNotBlank(dto.getType()), LotteryRuleAssignPO::getType, dto.getType())
                .eq(ObjectUtils.isNotEmpty(dto.getRoundId()),LotteryRuleAssignPO::getRoundId,dto.getRoundId())
                .like(StringUtils.isNotBlank(dto.getName()), LotteryRuleAssignPO::getName, dto.getName())
                .eq(StringUtils.isNotBlank(dto.getParkingLotCode()), LotteryRuleAssignPO::getParkingLotCode, dto.getParkingLotCode())
                .eq(StringUtils.isNotBlank(dto.getState()), LotteryRuleAssignPO::getState, dto.getState())
                .orderByAsc(LotteryRuleAssignPO::getCreateTm));

        List<LotteryRuleAssignBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), LotteryRuleAssignBO.class);

        //设置codeArr
        boList.forEach(lotteryRuleAssignBO -> lotteryRuleAssignBO.setCodeArr(JSON.parseObject(lotteryRuleAssignBO.getCode(), new TypeReference<List<String>>() {})));
        return PageUtils.toResponseList(page,boList);
    }


    /**
     * 新增摇号规则-停车场分配
     * @param dto
     * @return
     */
    @Override
    public Integer add(LotteryRuleAssignOptDTO dto) {
        //1.po生成
        LotteryRuleAssignPO po = new LotteryRuleAssignPO();
        BeanUtils.copyProperties(dto,po);
        po.setId(idWorker.nextId());
        po.setCreateTm(new Date());
        po.setUpdateTm(new Date());

        //2.属性设置
        propertySet(dto, po);

        //3.新增
        int result = mapper.insert(po);
        log.info("停车场分配新增成功  ——  {}",po);
        return result;
    }

    private void propertySet(LotteryRuleAssignOptDTO dto, LotteryRuleAssignPO po) {
        //2.属性设置
        //2.1根据轮数id设置轮数名称、停车场编号与区域
        LotteryRuleRoundPO roundPO = lotteryRuleRoundService.getById(dto.getRoundId());
        po.setRoundName(roundPO.getName());
        po.setParkingLotCode(roundPO.getParkingLotCode());
        po.setParkingLotRegion(parkingLotService.selectParkingLotByCode(roundPO.getParkingLotCode()).getRegion());
        //2.2设置部门编码/人员工号
        List<String> codeArr = dto.getCodeArr();
        po.setCode(JSON.toJSONString(codeArr));
        List<String> nameArr;
        if (RuleAssignTypeEnum.DEPARMENT.getState().equals(dto.getType())){
            nameArr = codeArr.stream().map(code -> departmentService.getDepartmentNameByDeptCode(code)).collect(Collectors.toList());
        }else {
        	List<UserProfilePO> profileList = userProfileService.getProfileListByJobNumList(codeArr);
            nameArr = profileList.stream().map(profile -> profile.getName()).collect(Collectors.toList());
        }
        po.setName(String.join(",", nameArr));
    }

    /**
     * 修改摇号规则-停车场分配
     * @param dto
     * @return
     */
    @Override
    public Integer update(LotteryRuleAssignOptDTO dto) {
        LotteryRuleAssignPO po = new LotteryRuleAssignPO();
        BeanUtils.copyProperties(dto,po);
        po.setUpdateTm(new Date());

        //2.属性设置
        propertySet(dto, po);
        int result = mapper.updateById(po);
        log.info("停车场分配修改成功  ——  {}",po);
        return result;
    }

    /**
     * 删除摇号规则-停车场分配
     * @param id
     * @return
     */
    @Override
    public Integer deleteById(Long id) {
        int result = mapper.deleteById(id);
        log.info("停车场分配删除成功，id：{}",id);
        return result;
    }

    /**
     * 根据roundId查询停车场区域名称
     * @param roundId
     * @return
     */
    @Override
    public String getParkingLotRegionByRoundId(Long roundId) {
        LotteryRuleRoundPO roundPO = lotteryRuleRoundService.getById(roundId);
        return  parkingLotService.selectParkingLotByCode(roundPO.getParkingLotCode()).getRegion();
    }

    /**
     * 人员导出
     * @param assignId
     * @return
     */
    @Override
    public List<LotteryRuleAssignExportBO> exportEmployee(Long assignId) {
        List<LotteryRuleAssignExportBO> boList = new ArrayList<>();
        LotteryRuleAssignPO assignPO = mapper.selectById(assignId);
        String parkingLotRegion = assignPO.getParkingLotRegion();
        String code = assignPO.getCode();
        List<String> codeList = JSON.parseObject(code, new TypeReference<List<String>>() {});
        List<String> jobNumList = new ArrayList<>();

        //1.如果是部门，查询对应部门下的人员
        if (RuleAssignTypeEnum.DEPARMENT.getState().equals(assignPO.getType())){
			/*
			 * LinkedHashSet<String> deptSet = new LinkedHashSet<>(codeList); //1.1添加所有子部门
			 * codeList.forEach(deptCode->{ addDeptCode(deptSet,deptCode); });
			 */
            //1.2.查询部门下的所有人员
            jobNumList = employeeService.queryEmployeeListByDept(codeList);

        }
        //2.如果是人员
        if (RuleAssignTypeEnum.EMPLOYEE.getState().equals(assignPO.getType())){
            jobNumList = codeList;
        }
        //3.根据工号list生成结果
        List<UserProfilePO> profileList = userProfileService.getProfileListByJobNumList(jobNumList);
        Map<String,UserProfilePO> map = profileList.stream().collect(Collectors.toMap(UserProfilePO::getJobNumber, each -> each, (value1,value2) -> value1 ));
        profileList.clear();
        jobNumList.stream().forEach(jobNum ->{
            UserProfilePO userProfilePO = map.get(jobNum);
            if (null != userProfilePO){
                LotteryRuleAssignExportBO bo = new LotteryRuleAssignExportBO().setCode(jobNum).setName(userProfilePO.getName()).setParkingLotRegion(parkingLotRegion);
                boList.add(bo);
            }
        });
        return boList;
    }

    private void addDeptCode(LinkedHashSet<String> deptSet, String deptCode) {
        List<DepartmentPO> poList = departmentService.getChildDepartmentByParentDeptCode(deptCode);
        if (!CollectionUtils.isEmpty(poList)){
            List<String> deptCodeList = poList.stream().map(DepartmentPO::getDeptCode).collect(Collectors.toList());
            deptSet.addAll(deptCodeList);
            deptCodeList.stream().forEach(code -> addDeptCode(deptSet,code));
        }
    }
}
