package com.cf.parking.api.response;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 用户基础列表
 * @author
 * @date 2023/9/13
 */
@Data
@ApiModel(description = "用户列表")
@Accessors(chain = true)
public class UserProfileBaseRsp {


    //部门编码
    @ApiModelProperty(value = "姓名")
    private String name;

    //工号
    @ApiModelProperty(value = "工号")
    private String code;
}
